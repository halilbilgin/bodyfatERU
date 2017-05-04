#!/bin/bash
for i in 1 4 7 10 13 16
do
        f=$i":"$(($i+2))
        test="run"$f

        screen -dmS $test Rscript cmdMethods.R -m "$f" -s "11:20"
done
