#
# Doom2D Forever spec file. It's a official build of editor for Linux.
# Maintainer: Dmitry Lyashuk <lyashuk.voxx@gmail.com>
#
# Copyright (C)  Doom 2D: Forever Developers
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 3 of the License ONLY.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

Name: d2dforever
Version: v0.667
Release: %mkrel 1
Summary: Doom-themed platformer with network play
Source0: doom2df-editor.png
Source1: doom2df_editor.desktop
URL: http://doom2d.org

BuildRequires: fpc lazarus

Group: Games/Arcade
License: GPLv3
Provides: d2df-editor

%description
Doom-themed platformer with network play, modern port of the 1996 Doom 2D by Prikol Software. Package contains main binary, headless binary and editor binary with icons and desktop files. Manual (man d2df) included.

%prep 

# Editor sources.
cd ..
git clone http://repo.or.cz/d2df-editor.git
cd d2df-editor
mkdir bin
mkdir tmp

%build 
cd d2df-editor/src/editor

# Editor bin.
export D2DF_BUILD_HASH="$(git rev-parse HEAD)"
lazbuild --bm=Debug Editor.lpi
  
%install

# Create directories.
install -dm755 %{buildroot}/{usr/{bin,share/pixmaps}}
install -dm755 %{buildroot}/usr/share/doom2df

# Installing files.
# Editor.
cd ../..
cd d2df-editor/bin
install -D -m755 editor %{buildroot}%{_bindir}/Editor

# Other.
install -D -m644 %{SOURCE1} %{buildroot}/usr/share/pixmaps/doom2df-editor.png
install -D -m644 %{SOURCE3} %{buildroot}/usr/share/applications/doom2df_editor.desktop

%clean
rm -Rf d2df-editor
rm -Rf %{buildroot}
  
%files 
%defattr(0755,root,root) 

# Binaries.
%{_bindir}/Editor

# Icons.
/usr/share/pixmaps/doom2df-editor.png

# Desktop files.
/usr/share/applications/doom2df_editor.desktop
