Process audio
-------------

~~~~ {.sh}
session_number=0
infile=RecordXXX.amr
outfile=fpb-${session_number}
title='There is no Spoon'
date='2015-02-16'

mplayer -really-quiet "${infile}" -ao pcm:file=>( sox -t wav - -t wav -e unsigned-integer "${outfile}.wav" )
# ... post process... for example Audacious
oggenc -q 4 \
    --date "${date}" \
    --title "${title}" \
    --album 'FPB' \
    --artist "Speaker ${session_number}" \
    --genre speach \
    "${outfile}.wav"
~~~~

Audio should start with informative message:

This is recording of Functional Programming Brno
meetup number `__` where `____` `____` talks
about `____`. Functional Programming Brno is platform
for everyone with interest in functional programming
that happens to be in or near the Brno. To find out
more visit <FPBrno.github.io>.

Prepare data for player
-----------------------

~~~~ {.sh}
n=0
import -window root img-$n.png; len n+=1
~~~~

or for `pdf`

~~~~ {.sh}
gs -dBATCH -dNOPAUSE -dSAFER \
    -dAlignToPixels=0 -dGridFitTT=2 \
    -dTextAlphaBits=4 -dGraphicsAlphaBits=4 \
    -dPDFFitPage -g1024x598 \
    -sOutputFile=slides/img-%d.png \
    -sDEVICE=png48 \
    "${slides}.pdf"
~~~~

Then minify screenshots at <https://tinypng.com/>.

Create online player
--------------------

For inspiration look to last meetup recording.

Other
-----

* create new folder "fbb-X"
* stuff it with data
* modify `gen/test-html.hs` and regenerate `index.html`
* check locally how it looks
* check that it will work over http and https
  (for example use `//` instead of `http://` where possible)
* create pull-request
