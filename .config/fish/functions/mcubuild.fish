function mcubuild -a 'workspace' 'configuration'
	if [ -n "$workspace" ]
		if [ $workspace = "Debug" ]; or [ $workspace = "Release" ]
			set BUILD_CONFIGURATION $workspace
		else
			set BUILD_WORKSPACE_PATH (realpath $workspace)
		end
	end
	if [ -n "$configuration" ]
		set BUILD_CONFIGURATION $configuration
	end

	if not set -q BUILD_WORKSPACE_PATH
		set BUILD_WORKSPACE_PATH (pwd)
	end
	if not set -q BUILD_CONFIGURATION
		set BUILD_CONFIGURATION "Debug"
	end

	if test -d $BUILD_WORKSPACE_PATH
		mcuxpresso -nosplash --launcher.suppressErrors -application org.eclipse.cdt.managedbuilder.core.headlessbuild -data $BUILD_WORKSPACE_PATH -build $BUILD_CONFIGURATION
	else
		echo $BUILD_WORKSPACE_PATH" is not a directory, exiting"
	end
end
