DEP_DIR="dependencies"
BUILD_DIR="build"
UPSTREAM="https://github.com/Cyan4973/xxHash.git"
XXHASH="$DEP_DIR/xxHash"
USAGE="USAGE: build.sh [release | debug | profiling]"

if [ ! -d $DEP_DIR ]; then mkdir -p $DEP_DIR; fi
if [ ! -d $BUILD_DIR ]; then mkdir -p $BUILD_DIR; fi
if [ ! -d $XXHASH ]
then 
    (cd $DEP_DIR || exit; git clone --depth=1 $UPSTREAM -o xxHash)
fi

(
    cd $BUILD_DIR || exit;
    cmake ..;

    if [ $# -eq 0 ]
    then
        make release
        cp skooma ../.
    elif [ $# -eq 1 ]
    then
        case $1 in
            "release" | "debug" | "profiling")
                make clean
                make "$1"
                ;;
            *)
                echo "Invalid configuration '$1'."
                echo "$USAGE"
                ;;
        esac
    else
        echo "Too many arguments provided."
        echo "$USAGE"
    fi
)
