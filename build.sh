DEP_DIR="dependencies"
BUILD_DIR="build"
UPSTREAM="https://github.com/Cyan4973/xxHash.git"
XXHASH="$DEP_DIR/xxHash"

if [ ! -d $DEP_DIR ]; then mkdir -p $DEP_DIR; fi
if [ ! -d $BUILD_DIR ]; then mkdir -p $BUILD_DIR; fi
if [ ! -d $XXHASH ]
then 
    (cd $DEP_DIR || exit; git clone $UPSTREAM -o xxHash)
fi

(cd $BUILD_DIR || exit; cmake ..; make)
