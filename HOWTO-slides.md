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
