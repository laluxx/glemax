#!/usr/bin/env python3
"""
Generate C source and header files for X11 color name lookup from rgb.txt
Usage: python3 generate_colors.py rgb.txt src/x11_colors
       (generates src/x11_colors.h and src/x11_colors.c)
"""

import sys
import re
import os

def normalize_name(name):
    """Convert color name to normalized form for comparison (lowercase, no spaces)"""
    return name.lower().replace(' ', '')

def srgb_to_linear(val):
    """Convert 8-bit sRGB value to linear float"""
    srgb = val / 255.0
    if srgb <= 0.04045:
        return srgb / 12.92
    else:
        return pow((srgb + 0.055) / 1.055, 2.4)

def main():
    if len(sys.argv) != 3:
        print("Usage: python3 generate_colors.py rgb.txt output_path", file=sys.stderr)
        print("Example: python3 generate_colors.py rgb.txt src/x11_colors", file=sys.stderr)
        sys.exit(1)
    
    rgb_file = sys.argv[1]
    output_base = sys.argv[2]
    header_file = output_base + ".h"
    source_file = output_base + ".c"
    
    colors = []
    
    with open(rgb_file, 'r') as f:
        for line in f:
            line = line.strip()
            if not line or line.startswith('!'):
                continue
            
            # Parse: "R G B\t\tname" or "R G B  name"
            parts = re.split(r'\s+', line, maxsplit=3)
            if len(parts) < 4:
                continue
            
            try:
                r = int(parts[0])
                g = int(parts[1])
                b = int(parts[2])
                name = parts[3]
                
                # Convert to linear color space
                r_linear = srgb_to_linear(r)
                g_linear = srgb_to_linear(g)
                b_linear = srgb_to_linear(b)
                
                # Store normalized name for sorting/lookup, original name for display
                normalized = normalize_name(name)
                colors.append((normalized, name, r_linear, g_linear, b_linear))
            except (ValueError, IndexError):
                continue
    
    # Remove exact duplicates only (same RGB values AND same name)
    # Keep all name variations like "ghost white" and "GhostWhite"
    seen = set()
    unique_colors = []
    for color in colors:
        # Use (name, r, g, b) as key to detect exact duplicates
        key = (color[1], color[2], color[3], color[4])
        if key not in seen:
            seen.add(key)
            unique_colors.append(color)
    
    # Sort alphabetically by original name (case-insensitive) for binary search
    unique_colors.sort(key=lambda x: x[1].lower())
    
    # Generate header file
    with open(header_file, 'w') as f:
        f.write("// Generated from rgb.txt - DO NOT EDIT\n")
        f.write("#ifndef X11_COLORS_H\n")
        f.write("#define X11_COLORS_H\n")
        f.write("\n")
        f.write("typedef struct {\n")
        f.write("    const char *name;\n")
        f.write("    float r, g, b;\n")
        f.write("} X11Color;\n")
        f.write("\n")
        f.write("const X11Color *lookup_x11_color(const char *name);\n")
        f.write("\n")
        f.write("#endif // X11_COLORS_H\n")
    
    # Generate source file
    with open(source_file, 'w') as f:
        f.write("// Generated from rgb.txt - DO NOT EDIT\n")
        f.write("// This file contains X11 color name definitions\n")
        f.write("\n")
        f.write("#include <stdlib.h>\n")
        f.write("#include <strings.h>\n")
        basename = os.path.basename(header_file)
        f.write(f'#include "{basename}"\n')
        f.write("\n")
        
        # Find max name length for alignment
        max_name_len = max(len(f'"{name}"') for _, name, _, _, _ in unique_colors)
        
        f.write(f"static const X11Color x11_colors[{len(unique_colors)}] = {{\n")
        
        for _, name, r, g, b in unique_colors:
            name_field = f'"{name}"'
            # Align on commas
            f.write(f'    {{{name_field:<{max_name_len}}, {r:.8f}f, {g:.8f}f, {b:.8f}f}},\n')
        
        f.write("};\n")
        f.write("\n")
        f.write(f"#define X11_COLOR_COUNT {len(unique_colors)}\n")
        f.write("\n")
        f.write("// Case-insensitive binary search for color name\n")
        f.write("static int x11_color_compare(const void *a, const void *b) {\n")
        f.write("    return strcasecmp((const char *)a, ((const X11Color *)b)->name);\n")
        f.write("}\n")
        f.write("\n")
        f.write("const X11Color *lookup_x11_color(const char *name) {\n")
        f.write("    return bsearch(name, x11_colors, X11_COLOR_COUNT,\n")
        f.write("                   sizeof(X11Color), x11_color_compare);\n")
        f.write("}\n")
    
    print(f"Generated {header_file} and {source_file}")
    print(f"Found {len(unique_colors)} unique color names")

if __name__ == '__main__':
    main()
