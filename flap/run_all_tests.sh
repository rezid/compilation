MAKE_OUTPUT="$(make)"

if [ "$?" -eq "0" ]
then
    echo " [make]           : success"
else
    echo "${MAKE_OUTPUT}"
    exit 0
fi

TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide1.target)"

if [ "$?" -eq "0" ]
then
    echo " [valide1.target] : success (check valide1.hopix)"
else
    echo "${TEST_OUTPUT}"
    exit 0
fi
