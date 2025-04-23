<div align="center">

<div style="float: right">
<img src="assets/uwu.png" width="200" align="right" />
</div>


*A modern, hardware-accelerated rewrite of Emacs in C and OpenGL*

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg)]()
[![OpenGL](https://img.shields.io/badge/OpenGL-4.6-red.svg)]()

[Installation](#installation) •
[Features](#features) •
[Documentation](#documentation) •

</div>

## Overview

![Screenshot](assets/screenshot.png)
![Screenshot2](assets/screenshot2.png)

For the complete list of keybinds, start reading the `keyCallback()` function in `main.c`.
we don't have recursive major modes yet.

## Installation

### Prerequisites
- FreeType
- Tree-sitter
- OpenGL 4.6+
- GLFW
- Lume engine

### Build Instructions

1. Clone and install the Lume library:
```bash
git clone https://github.com/laluxx/lume.git
cd lume
make && sudo make install
```

2. Clone build and run Glemax:
```bash
git clone https://github.com/laluxx/glemax.git
cd glemax
make && ./glemax
```

## TODO
idk look for them if you really care

## License
This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.
