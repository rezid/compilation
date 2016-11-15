MAKE_OUTPUT="$(make)"

if [ "$?" -eq "0" ]
then
    echo " [make] : success"
else
    echo "${MAKE_OUTPUT}"
    exit 0
fi

MD5="$(md5sum ./battery_testing/valide1.target)"
TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide1.target)"

if [ "$?" -eq "0" ]
then
    if [ "${MD5}" == "77ff9bcd9c0c4eac6ae28c1be110cb25  ./battery_testing/valide1.target" ]
    then
        echo " [valide1.target] : success (md5 valide) "
    else
	echo " [valide1.target] : Error (md5 not valide) "
    fi
else
    echo "${TEST_OUTPUT}"
    exit 0
fi


TEST_OUTPUT="$(./flap --source hopix -V true ./battery_testing/valide2.target)"

if [ "$?" -eq "0" ]
then
    echo " [valide2.target] : success"
else
    echo "${TEST_OUTPUT}"
    exit 0
fi
