#!/bin/sh

if_expr() {
    [ ! $(echo "$1" | bc) -eq 0 ]
}

soft_alter() {
    CUR_LEVEL=$(light) # https://github.com/haikarainen/light
    MIN_STEP=0.01

    CMD=$1
    DELTA=$2
    SCALE=$3

    case $CMD in
        inc)
            EXPR=$( (if_expr $CUR_LEVEL'<'$DELTA) &&
                       ((if_expr $CUR_LEVEL+$MIN_STEP'>'$CUR_LEVEL*$SCALE) &&
                           echo $CUR_LEVEL+$MIN_STEP ||
                           echo $CUR_LEVEL*$SCALE
                       ) ||
                       echo $CUR_LEVEL+$DELTA
                 ) ;;
        dec)
            EXPR=$( (if_expr $CUR_LEVEL'>='$DELTA*$SCALE) &&
                       ((if_expr $CUR_LEVEL'<='$MIN_STEP) &&
                           echo $CUR_LEVEL ||
                           echo $CUR_LEVEL-$DELTA
                       ) ||
                       echo $CUR_LEVEL/$SCALE
                 ) ;;
    esac
    NEW_LEVEL=$(printf "%.2f\n" $(awk "BEGIN { print "$EXPR" }"))
    echo $NEW_LEVEL
    return 0
}

if [ "$#" -ge 1 -a \
     "$#" -le 3 -a \
     \( "$1" = "inc" -o \
        "$1" = "dec" \) ];
then
    CMD=$1
    DELTA=${2:-5}
    SCALE=${3:-1.5}
    light -S $(soft_alter $CMD $DELTA $SCALE)
else
    echo "Usage:"
    echo "  sh $(basename "$0") [CMD] [DELTA:5] [SCALE:1.5]"
    echo "Parameters:"
    echo "  CMD    {inc, dec}"
    echo "  DELTA  Brightness alter value"
    echo "  SCALE  Brightness scale coefficient for soft altering"
    echo "         in case of small brightness"
fi
