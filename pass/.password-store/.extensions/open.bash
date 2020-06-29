# This is an extension for `pass` that decrypts a 'password' to a
# temporary file readable only by us and opens that file. This command
# is blocking, and will delete the file once the viewer closes. The
# viewer selected via xdg-open.

target="$1"
passfile="$PREFIX/$target.gpg"
# cmd_extensions already calls check_sneaky_paths for us

if ! [ -f "$passfile" ] ; then
    die "Error: $target is not in the password store."
fi

# extract the extension of the target, so that we can preserve it in
# the temp file
ext="$(echo "$target" | cut -d '.' -f2-)"
if [ "$ext" != "$target" -a -n "$ext" ]; then
    ext=".$ext"
else
    ext=""
fi

dest=$(mktemp --tmpdir "--suffix=$ext" pass.XXXXX)
$GPG -d "${GPG_OPTS[@]}" "$passfile" > "$dest"
xdg-open "$dest"
rm -f "$dest"
