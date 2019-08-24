# Example makefile, Solaris only
CC = gcc
CFLAGS=-O3 -g -fPIC -pedantic -Wall -I$(ERL_INCLUDE)
LD=ld
RM_RF=rm -rf
INSTALL_DIR=/usr/ucb/install -d
LIBRARIES=-lc -ltermlib -lresolv -ldl -lm  -lsocket -lnsl
TARGET_DIR=../priv/lib
OBJECT_DIR=../priv/obj
SHLIB_EXT=.so
OBJ_EXT=.o
TARGET_NAME=uds_drv$(SHLIB_EXT)
TARGET=$(TARGET_DIR)/$(TARGET_NAME)
OBJECTS=$(OBJECT_DIR)/uds_drv$(OBJ_EXT)

LDFLAGS=-G -h $(TARGET_NAME)

# Works if building in open source source tree
ERL_INCLUDE=$(ERL_TOP)/erts/emulator/beam

opt: setup $(OBJECTS)
	$(LD) $(LDFLAGS) $(OBJECTS) -o $(TARGET) $(LIBRARIES)

setup:
	$(INSTALL_DIR) $(TARGET_DIR)
	$(INSTALL_DIR) $(OBJECT_DIR)

$(OBJECT_DIR)/%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	$(RM_RF) $(TARGET_DIR) $(OBJECT_DIR)
