# ComicBake

A simple program for generating web comics from simple text scripts.

The comic author can concentrate on the script and let the program convert this dialogue into final output with speech bubbles.

## Introduction

On the assumption that many people want to make webcomics who have no artistic talents for graphics, especially mediated through computer software, this program lets you *write* a comic. The resulting script is processed and combined with pre-chosen images to give a webcomic-like result.

It is particularly aimed at/suited for comics which reuse the same images with different dialogue, eg Dinosaur Comics (the inspiration for this program and a damn fine read: <http://qwantz.com/>).

## How to use ComicBake

ComicBake is a command-line tool. In order to make a comic strip with it you will need:

*   Script
*   Background images
*   Image maps

The script includes the path to the images, and image maps are assumed to be similarly named to the images with the extension ".map".

Invoke ComicBake like this:

    $ comicbake build --input scripts/waiting.script

It will write out `comicstrip.png` (the final image) and a `comicstrip.N.png` panel file for each scene that comprises your final image. These are essentially temporary files so you can store them elsewhere:

    $ comicbake build --input scripts/waiting.script --tmpdir tmp/

If you want the output image to go somewhere different use this:

    $ comicbake build --input scripts/waiting.script --outdir ~/comics/

The root name of the output file can be changed from the default _comicstrip_ to something more descriptive. Note that the ".png" extension is added.

    $ comicbake build --input scripts/waiting.script --outdir ~/comics/ -o waitingforgodot

If you've got a Flickr account you can upload a comic with (optionally) a descriptive name, explanatory text and tags. The first time you upload something you'll have to authorise the program but it will happen automatically after that.

    $ comicbake publish --comic ~/comics/waitingforgodot.png --title "Waiting for Godot" --description "Based on the play by Samuel Beckett" --tags pretentious,"samuel beckett",godot

Use `comicbake --help` as a reminder.

## Writing a script

The script is a simple text file which contains a preamble and a number of scenes. I recommend you look at the contents of `examples/` to see what is possible.

The preamble is any number of comment lines (prefixed with "--") or blank lines. Comments can contain metadata in a "key: value" format, such as setting the title of the comic:

    -- title: Waiting for Godot
    -- Based on the play by Samuel Beckett.

Keys which the program does not recognised are ignored, as are ordinary comment lines, so you're basically free to write what you want here.

The scenes take the following general format:

    Scene N. <images/countryroad.png>

    Vladimir: That passed the time.

    Estragon: It would have passed
              in any case.

The image path, in angle brackets, is relative to the location of the script file. So the file `scripts/waiting.script` points to `scripts/images/countryroad.png`. Each scene is rendered as a separate image, a panel in traditional comic strip terms. If multiples scenes share the same background image then only the first in the series needs to have its image specified.

Each character's name must be spelled consistently though case doesn't matter. The text of their speeches is laid out on the image in the same format as it appears in the script, but ignoring whitespace at the start and end of lines. The following would render identically (sorry e.e. cummings)

    Romeo: But soft!
               what light 
                    through yonder 
                            window breaks?

    Romeo: But, soft!
           what light 
           through yonder 
           window breaks? 

Look at the examples and play around. Have fun.

## Image maps

It's necessary to let the computer know where the characters are, and since even facial recognition software can't really tell which person is which you have to do it explicitly.

The GIMP has a tool which lets you load in images, draw rectangles around items and save this as a snippet of HTML for client-side image maps. ComicBake will parse such files. Look at the `*.map` files in `examples/scenes/` to see what it expects. You shouldn't have to manually edit it if you do it right :-)

This is one of the uglier aspects of the whole process but thankfully it isn't a common one. Once the file is saved it can be reused indefinitely.

