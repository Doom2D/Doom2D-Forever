/* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#pragma once

#include <stdint.h>

#define MAP_MAGIC "MAP\x01"

enum {
  MBLK_NONE = 0,
  MBLK_TEXTURES = 1,
  MBLK_PANELS = 2,
  MBLK_HEADER = 7,
};

enum {
  PANEL_NONE = 0, // 0
  PANEL_WALL, // 1
  PANEL_BACK, // 2
  PANEL_FORE, // 4
  PANEL_WATER, // 8
  PANEL_ACID1, // 16
  PANEL_ACID2, // 32
  PANEL_STEP, // 64
  PANEL_LIFTUP, // 128
  PANEL_LIFTDOWN, // 256
  PANEL_OPENDOOR, // 512
  PANEL_CLOSEDOOR, // 1024
  PANEL_BLOCKMON, // 2048
  PANEL_LIFTLEFT, // 4096
  PANEL_LIFTRIGHT, // 8192

  PANEL_NUMTYPES
};

enum {
  PFLAG_HIDE = 2,
  PFLAG_WATERTEXTURES = 4,
};

#pragma pack(push, 1)

typedef struct {
  char name[32];
  char author[32];
  char desc[256];
  char music[64];
  char sky[64];
  uint16_t width;
  uint16_t height;
} map_header_t;

typedef struct {
  char resname[64];
  uint8_t anim;
} map_texture_t;

typedef struct {
  int32_t x, y;
  uint16_t w, h;
  uint16_t texid;
  uint16_t type;
  uint8_t alpha;
  uint8_t flags;
} map_panel_t;

typedef struct {
  uint8_t type;
  uint32_t reserved;
  uint32_t size;
} map_block_t;

#pragma pack(pop)
