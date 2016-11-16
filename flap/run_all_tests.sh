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

MD5="$(md5sum ./battery_testing/valide3.hopix)"
TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide3.target)"

if [ "$?" -eq "0" ]
then
    if [ "${MD5}" == "3c47e9dbaf7cd3723211cf671650baa7  ./battery_testing/valide3.hopix" ]
    then
        echo " [valide3.target] : success (md5 valide) "
    else
	echo " [valide3.target] : Error (md5 not valide) "
    fi
else
    echo "${TEST_OUTPUT}"
    exit 0
fi

MD5="$(md5sum ./battery_testing/valide4.hopix)"
TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide4.target)"

if [ "$?" -eq "0" ]
then
    if [ "${MD5}" == "e8559650a0e927ca0483f62f4c00df89  ./battery_testing/valide4.hopix" ]
    then
        echo " [valide4.target] : success (md5 valide) "
    else
	echo " [valide4.target] : Error (md5 not valide) "
    fi
else
    echo "${TEST_OUTPUT}"
    exit 0
fi

MD5="$(md5sum ./battery_testing/valide5.hopix)"
TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide5.target)"

if [ "$?" -eq "0" ]
then
    if [ "${MD5}" == "2fe71a0f2c9703e7829625b780b85ee4  ./battery_testing/valide5.hopix" ]
    then
        echo " [valide5.target] : success (md5 valide) "
    else
	echo " [valide5.target] : Error (md5 not valide) "
    fi
else
    echo "${TEST_OUTPUT}"
    exit 0
fi


TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide6.target)"

if [ "$?" -eq "0" ]
then
    echo " [valide6.target] : success"
else
    echo "${TEST_OUTPUT}"
    exit 0
fi
