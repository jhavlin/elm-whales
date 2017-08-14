inotifywait -m ../elm --format '%w%f' -e modify |
    while read file; do
        echo $file
	cd ../elm
        elm-make Main.elm --output ../site/js/elm.js
    done
