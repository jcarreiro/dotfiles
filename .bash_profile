# .bash_profile --
#     Sourced by bash for login shells. On OS X, that includes every new
#     terminal window; on most other UNIX systems, only the very first
#     shell you run is a login shell.

# Source .bashrc to get common definitions for login and non-login shells.
if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi

# Set up environment variables for fb4a development.
export ANDROID_SDK=/opt/android_sdk
export ANDROID_NDK_REPOSITORY=/opt/android_ndk
export ANDROID_HOME=${ANDROID_SDK}

# Set up our PATH. We keep each path on its own line to make it easy to add
# or remove paths. Each line is prepended to the existing PATH value, so the
# highest priority path is _last_ in the list.
PATH=$ANDROID_SDK/tools:$ANDROID_SDK/platform-tools:$PATH
PATH=$HOME/bin:$PATH
PATH=/usr/bin:$PATH
PATH=/usr/local/bin:$PATH
PATH=/usr/local/sbin:$PATH
export PATH

# Get some system info...
/usr/local/bin/archey -c

# Print something funny...
/usr/local/bin/fortune
