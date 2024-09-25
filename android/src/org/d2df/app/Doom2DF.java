/* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
 * This file is part of Doom2D Forever.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, version 3 of
 * the License ONLY.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 */

package org.d2df.app;

import android.content.Intent;
import android.app.Activity;
import android.os.Bundle;

import org.libsdl.app.SDL;
import org.libsdl.app.SDLActivity;

public class Doom2DF extends SDLActivity {

  @Override
  protected String[] getLibraries () {
    return new String[] {
      "crystax",
      "SDL2",
      "mpg123",
      "SDL2_mixer",
      "enet",
      "miniupnpc",
      "Doom2DF"
    };
  }

  @Override
  protected String[] getArguments () {
    Intent intent = getIntent();
    String value = intent.getStringExtra(Launcher.prefArgs);
    String[] args = value.split("\\s+");
    return args;
  }

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    CopyAssets.copyAssets(SDL.getContext(), "");
    CopyAssets.copyAssets(SDL.getContext(), "data");
    CopyAssets.copyAssets(SDL.getContext(), "data/models");
    CopyAssets.copyAssets(SDL.getContext(), "maps");
    CopyAssets.copyAssets(SDL.getContext(), "maps/megawads");
    CopyAssets.copyAssets(SDL.getContext(), "wads");
    CopyAssets.copyAssets(SDL.getContext(), "instruments");
    CopyAssets.copyAssets(SDL.getContext(), "timidity.cfg");
  }

  @Override
  protected void onDestroy() {
    super.onDestroy();

    /* This will fix bug #31 and may be #32 */
    System.exit(0);
  }
}
