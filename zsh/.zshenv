LOCAL_ZSHENV="$HOME/.zshenv-local"

# Load and export all environment variables from systemd user configuration
if [ -d ~/.config/environment.d ] ; then
    for f in ~/.config/environment.d/* ; do
        cat "$f" | while read line ; do
            source <(echo "$line")
            export "$(echo "$line" | cut -d '=' -f1)"
        done
    done
fi

# Source machine-dependent configuration if any
test -e "$LOCAL_ZSHENV" && source "$LOCAL_ZSHENV"

which vimb > /dev/null && export BROWSER=vimb || export BROWSER=firefox

export ANDROID_SDK_ROOT=/opt/android-sdk
export ANDROID_AVD_HOME=$XDG_CONFIG_HOME/.android/avd
