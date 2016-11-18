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
    if [ "${MD5}" == "1029b23dda838784e28eb11849d9a6ec  ./battery_testing/valide3.hopix" ]
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
    if [ "${MD5}" == "7bcfe621aea15c80f7bb2fed1ef0fe04  ./battery_testing/valide4.hopix" ]
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

MD5="$(md5sum ./battery_testing/valide6.hopix)"
TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide6.target)"

if [ "$?" -eq "0" ]
then
    if [ "${MD5}" == "326e04a0a14f3c1c96e7859438487b85  ./battery_testing/valide6.hopix" ]
    then
        echo " [valide6.target] : success (md5 valide) "
    else
	echo " [valide6.target] : Error (md5 not valide) "
    fi
else
    echo "${TEST_OUTPUT}"
    exit 0
fi

MD5="$(md5sum ./battery_testing/valide7.hopix)"
TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide7.target)"

if [ "$?" -eq "0" ]
then
    if [ "${MD5}" == "780c32a4eeffee87227cf030bc6e9703  ./battery_testing/valide7.hopix" ]
    then
        echo " [valide7.target] : success (md5 valide) "
    else
	echo " [valide7.target] : Error (md5 not valide) "
    fi
else
    echo "${TEST_OUTPUT}"
    exit 0
fi

MD5="$(md5sum ./battery_testing/valide8.hopix)"
TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide8.target)"

if [ "$?" -eq "0" ]
then
    if [ "${MD5}" == "ffd62f1cfc0bc1fd7b485039b20868f3  ./battery_testing/valide8.hopix" ]
    then
        echo " [valide8.target] : success (md5 valide) "
    else
	echo " [valide8.target] : Error (md5 not valide) "
    fi
else
    echo "${TEST_OUTPUT}"
    exit 0
fi

MD5="$(md5sum ./battery_testing/valide9.hopix)"
TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide9.target)"

if [ "$?" -eq "0" ]
then
    if [ "${MD5}" == "8eceb68fc03289d2ad691c5e56ff1fee  ./battery_testing/valide9.hopix" ]
    then
        echo " [valide9.target] : success (md5 valide) "
    else
	echo " [valide9.target] : Error (md5 not valide) "
    fi
else
    echo "${TEST_OUTPUT}"
    exit 0
fi


TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide10.target)"

if [ "$?" -eq "0" ]
then
    echo " [valide10.target] : success"
else
    echo "${TEST_OUTPUT}"
    exit 0
fi
