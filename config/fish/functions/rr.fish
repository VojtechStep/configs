function rr
    set tempfile (mktemp -t tmp.XXXXXX)
    /usr/bin/ranger --choosedir="$tempfile" (set -q $argv[0]; and pwd; or $argv)
    if begin; test -f "$tempfile"; and [ (cat -- "$tempfile") != (echo -n (pwd)) ]; end
    	echo (cat "$tempfile")
        cd (cat "$tempfile")
    end
    rm -f -- "$tempfile"
end
