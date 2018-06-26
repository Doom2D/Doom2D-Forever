package org.d2df.app;

import android.app.Activity;
import android.os.Bundle;

import org.libsdl.app.SDL;
import org.libsdl.app.SDLActivity;

public class Doom2DF extends SDLActivity {

	@Override
	protected String[] getLibraries() {
		return new String[] {
			"SDL2",
			"mpg123",
			"SDL2_mixer",
			"enet",
			"nanoGL",
			"Doom2DF"
		};
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
}
