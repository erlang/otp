/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2020-2024. All Rights Reserved.
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

extern "C"
{
#include "beam_common.h"
}

#if !(defined(WIN32) || defined(__APPLE__) || defined(__MACH__) ||             \
      defined(__DARWIN__))
#    define HAVE_GDB_SUPPORT
#endif

#ifdef HAVE_GDB_SUPPORT

enum jit_actions : uint32_t {
    JIT_NOACTION = 0,
    JIT_REGISTER_FN,
    JIT_UNREGISTER_FN,
};

struct jit_code_entry {
    struct jit_code_entry *next_entry;
    struct jit_code_entry *prev_entry;
    const char *symfile_addr;
    uint64_t symfile_size;
};

struct jit_descriptor {
    uint32_t version;
    jit_actions action_flag;
    struct jit_code_entry *relevant_entry;
    struct jit_code_entry *first_entry;
    erts_mtx_t mutex;
};

extern "C"
{
    extern void ERTS_NOINLINE __jit_debug_register_code(void);

    /* Make sure to specify the version statically, because the
     * debugger may check the version before we can set it. */
    struct jit_descriptor __jit_debug_descriptor = {1,
                                                    JIT_NOACTION,
                                                    NULL,
                                                    NULL};
} /* extern "C" */

struct emulator_info {
    /* 0 = regular, 1 = frame pointers */
    int frame_layout;
    const void *normal_exit;
};

struct erlang_module_info {
    uint64_t base_address;
    uint32_t range_count;
    uint32_t code_size;
    /* Module name, including null terminator. */
    uint16_t name_length;
    char name[];
};

struct range_info {
    uint32_t start_offset;
    uint32_t end_offset;
    uint32_t line_count;
    /* Range name, including null terminator. */
    uint16_t name_length;
    char name[];
};

struct line_info {
    uint32_t start_offset;
    uint32_t line_number;
    /* File name, including null terminator. */
    uint16_t file_length;
    char file[];
};

enum debug_info_header {
    DEBUG_INFO_HEADER_EMULATOR = 0,
    DEBUG_INFO_HEADER_MODULE = 1,
};

struct debug_info {
    enum debug_info_header header;
    union {
        struct emulator_info emu;
        struct erlang_module_info mod;
    } payload;
};

static void beamasm_init_early_gdb() {
    erts_mtx_init(&__jit_debug_descriptor.mutex,
                  "jit_debug_descriptor",
                  NIL,
                  (ERTS_LOCK_FLAGS_PROPERTY_STATIC |
                   ERTS_LOCK_FLAGS_CATEGORY_GENERIC));
}

static void beamasm_init_late_gdb() {
    auto debug_info = (struct debug_info *)malloc(sizeof(struct debug_info));
    jit_code_entry *entry;

    debug_info->header = DEBUG_INFO_HEADER_EMULATOR;
    debug_info->payload.emu.frame_layout = erts_frame_layout;
    debug_info->payload.emu.normal_exit = beam_normal_exit;

    entry = (jit_code_entry *)malloc(sizeof(jit_code_entry));
    entry->symfile_addr = (char *)debug_info;
    entry->symfile_size = sizeof(struct debug_info);

    /* Insert into linked list */
    erts_mtx_lock(&__jit_debug_descriptor.mutex);

    if (__jit_debug_descriptor.first_entry) {
        ASSERT(__jit_debug_descriptor.first_entry->prev_entry == nullptr);
        __jit_debug_descriptor.first_entry->prev_entry = entry;
    }

    entry->prev_entry = nullptr;
    entry->next_entry = __jit_debug_descriptor.first_entry;
    __jit_debug_descriptor.first_entry = entry;

    /* Register with gdb */
    ASSERT(__jit_debug_descriptor.relevant_entry == nullptr);
    __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;
    __jit_debug_descriptor.relevant_entry = entry;
    __jit_debug_register_code();
    __jit_debug_descriptor.relevant_entry = nullptr;

    erts_mtx_unlock(&__jit_debug_descriptor.mutex);
}

static void *beamasm_insert_gdb_info(std::string module_name,
                                     ErtsCodePtr base_address,
                                     size_t code_size,
                                     const std::vector<AsmRange> &ranges) {
    Sint symfile_size = sizeof(struct debug_info) + module_name.size() + 1;

    for (const auto &range : ranges) {
        symfile_size += sizeof(struct range_info) + range.name.size() + 1;

        for (const auto &line : range.lines) {
            symfile_size += sizeof(struct line_info) + line.file.size() + 1;
        }
    }

    char *symfile = (char *)malloc(symfile_size);
    jit_code_entry *entry;

    entry = (jit_code_entry *)malloc(sizeof(jit_code_entry));
    entry->symfile_addr = symfile;
    entry->symfile_size = symfile_size;

    auto debug_info = (struct debug_info *)symfile;
    debug_info->header = DEBUG_INFO_HEADER_MODULE;

    auto module_info = &debug_info->payload.mod;
    module_info->base_address = (uint64_t)base_address;
    module_info->code_size = (uint32_t)code_size;
    module_info->range_count = ranges.size();
    module_info->name_length = module_name.size() + 1;
    sys_memcpy(module_info->name,
               module_name.c_str(),
               module_info->name_length);

    symfile += sizeof(*debug_info) + module_info->name_length;

    for (const auto &range : ranges) {
        ASSERT(range.start >= base_address && range.start <= range.stop);

        auto range_info = (struct range_info *)symfile;
        range_info->start_offset = (char *)range.start - (char *)base_address;
        range_info->end_offset = (char *)range.stop - (char *)base_address;
        range_info->line_count = (uint32_t)range.lines.size();
        range_info->name_length = (uint16_t)range.name.size() + 1;
        sys_memcpy(range_info->name,
                   range.name.c_str(),
                   range_info->name_length);

        symfile += sizeof(*range_info) + range_info->name_length;

        for (const auto &line : range.lines) {
            auto line_info = (struct line_info *)symfile;
            line_info->start_offset = (char *)line.start - (char *)base_address;
            line_info->line_number = (uint32_t)line.line;
            line_info->file_length = (uint16_t)line.file.size() + 1;
            sys_memcpy(line_info->file,
                       line.file.c_str(),
                       line_info->file_length);

            symfile += sizeof(*line_info) + line_info->file_length;
        }
    }

    ASSERT(symfile_size == (symfile - entry->symfile_addr));

    /* Insert into linked list */
    erts_mtx_lock(&__jit_debug_descriptor.mutex);

    if (__jit_debug_descriptor.first_entry) {
        ASSERT(__jit_debug_descriptor.first_entry->prev_entry == nullptr);
        __jit_debug_descriptor.first_entry->prev_entry = entry;
    }

    entry->prev_entry = nullptr;
    entry->next_entry = __jit_debug_descriptor.first_entry;
    __jit_debug_descriptor.first_entry = entry;

    /* register with gdb */
    ASSERT(__jit_debug_descriptor.relevant_entry == nullptr);
    __jit_debug_descriptor.action_flag = JIT_REGISTER_FN;
    __jit_debug_descriptor.relevant_entry = entry;
    __jit_debug_register_code();
    __jit_debug_descriptor.relevant_entry = nullptr;

    erts_mtx_unlock(&__jit_debug_descriptor.mutex);

    /* Return a reference to our debugger entry so we can remove it when
     * unloading. */
    return entry;
}

static void beamasm_delete_gdb_info(void *metadata) {
    jit_code_entry *entry = (jit_code_entry *)metadata;

    erts_mtx_lock(&__jit_debug_descriptor.mutex);

    /* Unlink current module from the entry list. */
    if (entry->prev_entry) {
        entry->prev_entry->next_entry = entry->next_entry;
    } else {
        __jit_debug_descriptor.first_entry = entry->next_entry;
    }

    if (entry->next_entry) {
        entry->next_entry->prev_entry = entry->prev_entry;
    }

    /* unregister with gdb  */
    ASSERT(__jit_debug_descriptor.relevant_entry == nullptr);
    __jit_debug_descriptor.action_flag = JIT_UNREGISTER_FN;
    __jit_debug_descriptor.relevant_entry = entry;
    __jit_debug_register_code();
    __jit_debug_descriptor.relevant_entry = nullptr;

    erts_mtx_unlock(&__jit_debug_descriptor.mutex);

    free((void *)entry->symfile_addr);
    free(entry);
}
#endif /* HAVE_GDB_SUPPORT */

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

    void update(const std::vector<AsmRange> &ranges) {
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
        } record;

        record.header.id = JIT_CODE_LOAD;
        record.pid = getpid();
        record.tid = erts_thr_self();

        for (const AsmRange &range : ranges) {
            /* Line entries must be written first, if present. */
            if (!range.lines.empty()) {
                struct JitCodeDebugEntry {
                    Uint64 line_addr;
                    Uint32 line;
                    Uint32 column;
                    char name[];
                } debug_entry;

                struct JitCodeDebugInfoRecord {
                    RecordHeader header;
                    Uint64 code_addr;
                    Uint64 nr_entry;
                    struct JitCodeDebugEntry entries[];
                } debug_info;

                debug_info.header.id = JIT_CODE_DEBUG_INFO;
                debug_info.header.total_size = sizeof(debug_info);
                debug_info.header.timestamp = erts_os_monotonic_time();
                debug_info.code_addr = (Uint64)range.start;
                debug_info.nr_entry = range.lines.size() + 1;

                for (const auto &line : range.lines) {
                    debug_info.header.total_size += sizeof(debug_entry);
                    debug_info.header.total_size += line.file.size() + 1;
                }

                /* Add a dummy line to terminate the function. Otherwise, the
                 * line section will stop right before the final line. */
                debug_info.header.total_size += sizeof(debug_entry);
                debug_info.header.total_size += 1;

                fwrite(&debug_info, sizeof(debug_info), 1, file);

                for (const auto &line : range.lines) {
                    debug_entry.line_addr = (Uint64)line.start;
                    debug_entry.line = line.line;
                    debug_entry.column = 0;

                    fwrite(&debug_entry, sizeof(debug_entry), 1, file);
                    fwrite(line.file.c_str(), line.file.size() + 1, 1, file);
                }

                debug_entry.line_addr = (Uint64)range.stop;
                debug_entry.line = 0;
                debug_entry.column = 0;

                fwrite(&debug_entry, sizeof(debug_entry), 1, file);
                fwrite("", 1, 1, file);
            }

            ptrdiff_t codeSize = (byte *)range.stop - (byte *)range.start;
            size_t nameLen = range.name.size();

            ASSERT(codeSize >= 0);

            record.header.total_size = sizeof(record) + nameLen + 1 + codeSize;
            record.vma = (Uint64)range.start;
            record.code_addr = (Uint64)range.start;
            record.code_size = (Uint64)codeSize;
            record.code_index = ++code_index;
            record.header.timestamp = erts_os_monotonic_time();

            fwrite(&record, sizeof(record), 1, file);
            fwrite(range.name.c_str(), nameLen + 1, 1, file);
            fwrite(range.start, codeSize, 1, file);
        }

        fflush(file);
    }
};

#    endif /* HAVE_LINUX_PERF_SUPPORT */

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

    void update(const std::vector<AsmRange> &ranges) {
        for (const auto &range : ranges) {
            char *start = (char *)range.start, *stop = (char *)range.stop;
            ptrdiff_t size = stop - start;
            fprintf(file, "%p %tx $%s\n", start, size, range.name.c_str());
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

    void update(const std::vector<AsmRange> &ranges) {
        if (modes) {
            erts_mtx_lock(&mutex);
#    ifdef HAVE_LINUX_PERF_DUMP_SUPPORT
            if (modes & DUMP) {
                perf_dump.update(ranges);
            }
#    endif
            if (modes & MAP) {
                perf_map.update(ranges);
            }
            erts_mtx_unlock(&mutex);
        }
    }
};

static JitPerfSupport perf;
#endif /* HAVE_LINUX_PERF_SUPPORT */

void beamasm_metadata_early_init() {
#ifdef HAVE_GDB_SUPPORT
    beamasm_init_early_gdb();
#endif

#ifdef HAVE_LINUX_PERF_SUPPORT
    perf.init();
#endif
}

void beamasm_metadata_late_init() {
#ifdef HAVE_GDB_SUPPORT
    beamasm_init_late_gdb();
#endif
}

void *beamasm_metadata_insert(std::string module_name,
                              ErtsCodePtr base_address,
                              size_t code_size,
                              const std::vector<AsmRange> &ranges) {
#ifdef HAVE_LINUX_PERF_SUPPORT
    perf.update(ranges);
#endif

#ifdef HAVE_GDB_SUPPORT
    return beamasm_insert_gdb_info(module_name,
                                   base_address,
                                   code_size,
                                   ranges);
#else
    return NULL;
#endif
}

void beamasm_unregister_metadata(void *metadata) {
#ifdef HAVE_GDB_SUPPORT
    beamasm_delete_gdb_info(metadata);
#endif
}
