function mcuflash -a 'axffile'
	if not set -q MCU_IDE_DIR
		echo "Set the MCU_IDE_DIR environment variable to the root directory of your MCUXpresso IDE installation"
		return
	end

	eval $MCU_IDE_DIR/ide/bin/crt_emu_cm_redlink --flash-load-exec ""(realpath $axffile) -p PN7462AU-C3-00 --ConnectScript "$MCU_IDE_DIR/ide/bin/Scripts/PN7xxxxx_Connect.scp" --flash-driver "$MCU_IDE_DIR/ide/bin/Flash/PN7xxxxx_158k.cfx"
end
