#!/usr/bin/env python

# Pixel art dither script for GIMP
# Author: ahefner@gmail.com

import math
from gimpfu import *
from array import array

def python_8bit_dither(img, srclayer, x_offset, y_offset, x_chk, y_chk):
    pdb.gimp_image_undo_group_start(img)
    try:
        layer = srclayer.copy()
        img.add_layer(layer, 0)
        width = layer.width
        height = layer.height

        rgn = layer.get_pixel_rgn(0, 0, width, height, TRUE, FALSE)
        src_pixels = array("B", rgn[0:width, 0:height])

#        cmbytes, cmdata = pdb.gimp_image_get_colormap(img)
#        colors = cmbytes / 3
#        pdb.gimp_message(str(cmdata))

        i = 0
        ph = 0
        for y in range(height):
            ph = (y*y_chk + y_offset) & 1
            if (not y_chk): ph = 0
            for x in range(width):
                c = src_pixels[i]
                if (c & 1):
                    c = c-1 if (((ph + x_offset) & 1) == 0) else c+1
                if (c > 255):
                    c = 255
                src_pixels[i] = c
                if x_chk: ph = ph ^ 1
                i = i + 1

        rgn[0:width, 0:height] = src_pixels.tostring()

        layer.flush()
        layer.update(0,0,width,height)
        pdb.gimp_image_undo_group_end(img)

    except Exception, err:
        pdb.gimp_message("ERR: " + str(err))
        pdb.gimp_image_undo_group_end(img)

register(
        "python_fu_dither",
        "Dither odd colors in an indexed-color image",
        "Dither odd colors in an indexed-color image",
        "Andy Hefner",
        "Andy Hefner",
        "2013",
        "<Image>/Filters/Hefner/8-Bit Dither Helper",
        "INDEXED",
        [
         (PF_TOGGLE, "x_offset", "Pattern X offset", False),
         (PF_TOGGLE, "y_offset", "Pattern Y offset", False),
         (PF_TOGGLE, "x_chk", "X checkerboard", True),
         (PF_TOGGLE, "y_chk", "Y checkerboard", True)
        ],
        [],
        python_8bit_dither)

main()
