MAKE_OUTPUT="$(make)"

if [ "$?" -eq "0" ]
then
    echo " [make] : success"
else
    echo "${MAKE_OUTPUT}"
    exit 0
fi

MD5="$(md5sum ./battery_testing/valide1.hopix)"
TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide1.target)"

if [ "$?" -eq "0" ]
then
    if [ "${MD5}" == "5f0bf0c4c49c53d6ae760a8f7c904c6b  ./battery_testing/valide1.hopix" ]
    then
        echo " [valide1.target] : success (md5 valide) "
    else
	echo " [valide1.target] : Error (md5 not valide) "
    fi
else
    echo "${TEST_OUTPUT}"
    exit 0
fi

MD5="$(md5sum ./battery_testing/valide2.hopix)"
TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide2.target)"

if [ "$?" -eq "0" ]
then
    if [ "${MD5}" == "4bd01ecdf6021fc504247a92f5adf990  ./battery_testing/valide2.hopix" ]
    then
        echo " [valide2.target] : success (md5 valide) "
    else
	echo " [valide2.target] : Error (md5 not valide) "
    fi
else
    echo "${TEST_OUTPUT}"
    exit 0
fi

TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide3.target)"

if [ "$?" -eq "0" ]
then
    echo " [valide3.target] : success"
else
    echo "${TEST_OUTPUT}"
    exit 0
fi
