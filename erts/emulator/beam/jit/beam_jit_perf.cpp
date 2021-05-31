/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2020. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

#include "beam_asm.hpp"

#ifdef HAVE_LINUX_PERF_SUPPORT

#    ifdef HAVE_ELF_H
#        include <elf.h>
#        define HAVE_LINUX_PERF_DUMP_SUPPORT 1

class JitPerfDump {
    FILE *file = nullptr;
    Uint64 code_index = 0;

    enum PerfDumpId {
        JIT_CODE_LOAD = 0, /* record describing a jitted function */
        JIT_CODE_MOVE = 1, /* record describing an already jitted function which
                              is moved */
        JIT_CODE_DEBUG_INFO = 2, /* record describing the debug information for
                                    a jitted function */
        JIT_CODE_CLOSE =
                3, /* record marking the end of the jit runtime (optional) */
        JIT_CODE_UNWINDING_INFO =
                4 /* record describing a function unwinding information */
    };

    struct FileHeader {
        Uint32 magic;
        Uint32 version;
        Uint32 total_size;
        Uint32 elf_mach;
        Uint32 pad1;
        Uint32 pid;
        Uint64 timestamp;
        Uint64 flags;

        FileHeader() {
            magic = 0x4A695444; /* "JiTD" as numbers */
            version = 1;
            total_size = sizeof(FileHeader);
            elf_mach = EM_X86_64;
            pad1 = 0;
            pid = getpid();
            timestamp = erts_os_monotonic_time();
            flags = 0;
            ERTS_CT_ASSERT(sizeof(FileHeader) == 4 * 10);
        }
    };

    struct RecordHeader {
        Uint32 id;
        Uint32 total_size;
        Uint64 timestamp;
    };

public:
    bool init() {
        char name[MAXPATHLEN];
        FileHeader header;

        /* LLVM places this file in ~/.debug/jit/ maybe we should do that to? */

        erts_snprintf(name, sizeof(name), "/tmp/jit-%d.dump", getpid());

        file = fopen(name, "w+");

        if (file) {
            fwrite(&header, sizeof(header), 1, file);
            /* inform perf of the location of the dump file */
            void *addr = mmap(NULL,
                              sizeof(header),
                              PROT_READ | PROT_EXEC,
                              MAP_PRIVATE,
                              fileno(file),
                              0);
            if (addr == MAP_FAILED) {
                int saved_errno = errno;
                erts_dsprintf_buf_t *dsbuf = erts_create_logger_dsbuf();
                erts_dsprintf(dsbuf,
                              "perf: mmap of %s(%d) failed: %d\r\n",
                              name,
                              fileno(file),
                              saved_errno);
                erts_send_error_to_logger_nogl(dsbuf);
                fclose(file);
                file = nullptr;
                return false;
            }
        } else {
            int saved_errno = errno;
            erts_dsprintf_buf_t *dsbuf = erts_create_logger_dsbuf();
            erts_dsprintf(dsbuf,
                          "perf: Failed to open %s (%d)",
                          name,
                          saved_errno);
            erts_send_error_to_logger_nogl(dsbuf);
            return false;
        }
        return true;
    }

    void update_perf_info(std::string modulename,
                          std::vector<BeamAssembler::AsmRange> &ranges) {
        struct JitCodeLoadRecord {
            RecordHeader header;
            Uint32 pid;
            Uint32 tid;
            Uint64 vma;
            Uint64 code_addr;
            Uint64 code_size;
            Uint64 code_index;
            /* Null terminated M:F/A */
            /* Native code */
        };
        JitCodeLoadRecord record;
        record.header.id = JIT_CODE_LOAD;
        record.pid = getpid();
        record.tid = erts_thr_self();
        for (BeamAssembler::AsmRange &r : ranges) {
            size_t nameLen = r.name.size();
            ptrdiff_t codeSize = (char *)r.stop - (char *)r.start;
            ASSERT(codeSize >= 0);
            record.header.total_size = sizeof(record) + nameLen + 1 + codeSize;
            record.vma = (Uint64)r.start;
            record.code_addr = (Uint64)r.start;
            record.code_size = (Uint64)codeSize;
            record.code_index = ++code_index;
            record.header.timestamp = erts_os_monotonic_time();

            fwrite(&record, sizeof(record), 1, file);
            fwrite(r.name.c_str(), nameLen + 1, 1, file);
            fwrite(r.start, codeSize, 1, file);
        }
    }
};

#    endif

class JitPerfMap {
    FILE *file = nullptr;

public:
    bool init() {
        char name[MAXPATHLEN];
        snprintf(name, sizeof(name), "/tmp/perf-%i.map", getpid());
        file = fopen(name, "w");
        if (!file) {
            int saved_errno = errno;
            erts_dsprintf_buf_t *dsbuf = erts_create_logger_dsbuf();
            erts_dsprintf(dsbuf,
                          "perf: Failed to open %s (%d)",
                          name,
                          saved_errno);
            erts_send_error_to_logger_nogl(dsbuf);
            return false;
        }
        return true;
    }
    void update_perf_info(std::string modulename,
                          std::vector<BeamAssembler::AsmRange> &ranges) {
        for (BeamAssembler::AsmRange &r : ranges) {
            char *start = (char *)r.start, *stop = (char *)r.stop;
            ptrdiff_t size = stop - start;
            fprintf(file, "%p %tx $%s\n", start, size, r.name.c_str());
        }
        fflush(file);
    }
};

class JitPerfSupport {
    enum PerfModes { NONE = 0, MAP = (1 << 0), DUMP = (1 << 1) };

    int modes;

    erts_mtx_t mutex;
#    ifdef HAVE_LINUX_PERF_DUMP_SUPPORT
    JitPerfDump perf_dump;
#    endif
    JitPerfMap perf_map;

public:
    JitPerfSupport() : modes(NONE) {
    }
    void init() {
        modes = JitPerfSupport::NONE;
#    ifdef HAVE_LINUX_PERF_DUMP_SUPPORT
        if ((erts_jit_perf_support & BEAMASM_PERF_DUMP) && perf_dump.init()) {
            modes |= JitPerfSupport::DUMP;
        }
#    endif
        if ((erts_jit_perf_support & BEAMASM_PERF_MAP) && perf_map.init()) {
            modes |= JitPerfSupport::MAP;
        }

        erts_mtx_init(&mutex,
                      "perf",
                      NIL,
                      ERTS_LOCK_FLAGS_PROPERTY_STATIC |
                              ERTS_LOCK_FLAGS_CATEGORY_GENERIC);
    }

    void update_perf_info(std::string modulename,
                          std::vector<BeamAssembler::AsmRange> &ranges) {
        if (modes) {
            erts_mtx_lock(&mutex);
#    ifdef HAVE_LINUX_PERF_DUMP_SUPPORT
            if (modes & DUMP) {
                perf_dump.update_perf_info(modulename, ranges);
            }
#    endif
            if (modes & MAP) {
                perf_map.update_perf_info(modulename, ranges);
            }
            erts_mtx_unlock(&mutex);
        }
    }
};

static JitPerfSupport perf;

void beamasm_init_perf() {
    perf.init();
}

void beamasm_update_perf_info(std::string modulename,
                              std::vector<BeamAssembler::AsmRange> &ranges) {
    perf.update_perf_info(modulename, ranges);
}
#else
void beamasm_init_perf() {
}

void beamasm_update_perf_info(std::string modulename,
                              std::vector<BeamAssembler::AsmRange> &ranges) {
    (void)modulename;
    (void)ranges;
}
#endif
