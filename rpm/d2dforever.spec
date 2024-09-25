#
# Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
# Doom2D Forever spec file. It's an official build of game for Linux.
# Maintainer: Dmitry Lyashuk <lyashuk.voxx@gmail.com>
#
# This program is free software: you can redistribute it and/or modify it under the terms of
# the GNU General Public License as published by the Free Software Foundation, version 3 of
# the License ONLY.

# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details.

# You should have received a copy of the GNU General Public License along with this program.
# If not, see <http://www.gnu.org/licenses/>.
#

Name: d2dforever
Version: v0.667
Release: %mkrel 1
Summary: a Doom-themed platformer with online multiplayer
Source0: doom2df.png
Source1: doom2df.desktop
Source2: d2df.1
URL: http://doom2d.org

BuildRequires: fpc libenet-devel libvorbis-devel libopus-devel libopusfile-devel libgme-devel libxmp-devel libopenal-devel libsdl2.0-devel

Group: Games/Arcade
License: GPLv3
Requires: libenet7 libsdl2.0_0 openal libvorbis0 libopus0 libopusfile0 libgme0 libxmp4
Provides: d2dforever

%description
A Doom-themed platformer with online multiplayer, modern remake of the 1996 game Doom2D by Prikol Software. Package contains main binary, headless binary and editor binary with icons and desktop files. Manual (man d2df) included.

%prep 

# Game sources.
git clone http://repo.or.cz/d2df-sdl.git
cd d2df-sdl
mkdir bin
mkdir tmp

%build 
cd d2df-sdl/src/game

# Main bin.
export D2DF_BUILD_HASH="$(git rev-parse HEAD)"
fpc -g -gl -FE../../bin -FU../../tmp -dUSE_SDL2 -dUSE_OPENGL -dUSE_OPENAL -dUSE_SDL2 -dUSE_XMP -dUSE_VORBIS -dUSE_OPUS -dUSE_GME Doom2DF.lpr

# Headless bin.
cd ../..
rm -R tmp
mkdir tmp
cd src/game
fpc -g -gl -FE../../bin -FU../../tmp -dUSE_SYSSTUB -dUSE_OPENAL -dUSE_XMP -dUSE_VORBIS -dUSE_MODPLUG -dUSE_OPUS -dUSE_GME -dHEADLESS -oDoom2DF_H Doom2DF.lpr

%install

# Create directories.
install -dm755 %{buildroot}/{usr/{bin,share/pixmaps}}
install -dm755 %{buildroot}/usr/share/doom2df

# Installing files.
# Game.
cd d2df-sdl/bin
install -D -m755 Doom2DF %{buildroot}%{_bindir}/Doom2DF
install -D -m755 Doom2DF_H %{buildroot}%{_bindir}/Doom2DF_H

# Other.
install -D -m644 %{SOURCE0} %{buildroot}/usr/share/pixmaps/doom2df.png
install -D -m644 %{SOURCE2} %{buildroot}/usr/share/applications/doom2df.desktop
install -D -m644 %{SOURCE4} %{buildroot}%{_mandir}/man1/d2df.1

%clean
rm -Rf d2df-sdl
rm -Rf %{buildroot}
  
%files 
%defattr(0755,root,root) 

# Binaries.
%{_bindir}/Doom2DF
%{_bindir}/Doom2DF_H

# Icons.
/usr/share/pixmaps/doom2df.png

# Desktop files.
/usr/share/applications/doom2df.desktop

# Man pages.
%{_mandir}/man1/d2df.1*
