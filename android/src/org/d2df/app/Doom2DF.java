package org.d2df.app;

import android.app.Activity;
import android.os.Bundle;

import org.libsdl.app.SDLActivity;

public class Doom2DF extends SDLActivity {

	@Override
	protected String[] getLibraries() {
		return new String[] {
			"SDL2",
			"SDL2_mixer",
			"enet",
			"nanoGL",
			"Doom2DF"
		};
	}
}
