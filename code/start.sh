#!/bin/bash
# http://stackoverflow.com/questions/9023164/in-bash-how-can-i-run-multiple-infinitely-running-commands-and-cancel-them-all

# go to the directory of the script first
cd "$(dirname "$0")"

args=("$@")
verbose="TRUE"
output=0
kindly="FALSE"
help="FALSE"
terminate="FALSE"
for arg in "${args[@]}"; do
  if [ $arg == "-q" -o $arg == "--quiet" ]; then
	  verbose="FALSE"
	elif [ $arg == "-r" -o $arg == "--return-pid" ]; then
	  output=1
	elif [ $arg == "-k" -o $arg == "--kindly" ]; then
	  kindly="TRUE"
	elif [ $arg == "-h" -o $arg == "--help" ]; then
	  help="TRUE"
	else
		terminate="TRUE"
		echo "start.sh: illegal option, use -h or --help for help"
		echo "usage: ./start.sh [-qrkh]"
	fi
done

if [ $terminate != "TRUE" ]; then
	if [ $help == "TRUE" ]; then
		echo ""
		echo "Usage: ./start.sh [Options]"
		echo ""
		echo "Start the app servers, including the multi-purpose server controller at app.R"
		echo "and the interactive-graphic apps that requires GUI attached to headless server."
		echo "If the process is interupted (Ctrl+C), all the apps and processes listening to"
		echo "the ports which are supposed to be used by the apps will be killed violently."
		echo ""
		echo "Options:"
		echo "  -h, --help              Print short help message and exit"
		echo "  -k, --kindly            Do not kill the processes listening to the ports used by the apps"
		echo "  -q, --quiet             Do not print any messages"
		echo "  -r, --return-pid        Return the pids used by the script"
	else
		# There is some problem with apps requiring GUI device or installation of certain packages
		R CMD BATCH --vanilla --slave apps/pca3d.R /dev/null &
		PIDS[0]=$!

		R CMD BATCH --vanilla --slave apps/visnetwork.category_by_support.R /dev/null &
		PIDS[1]=$!

		R CMD BATCH --vanilla --slave apps/visnetwork.category_by_confidence.R /dev/null &
		PIDS[2]=$!

		R CMD BATCH --vanilla --slave apps/visnetwork.category_by_lift.R /dev/null &
		PIDS[3]=$!

		R CMD BATCH --vanilla --slave apps/visnetwork.pca.R /dev/null &
		PIDS[4]=$!

		# Other Apps
		# depends on the machine, the output can be /dev/tty0
		# specify and output like this allows output to the console, use /dev/null for nothing
		R CMD BATCH --vanilla --slave "--args $verbose" start.R /dev/tty &
		PIDS[5]=$!

		# Set output
		if [ $output -eq 1 ]; then
			printf '%s\n' "${PIDS[@]}"
		fi

		# To kill or not to kill
		if [ $kindly == "TRUE" ]; then
			trap "kill ${PIDS[*]}; kill -KILL ${PIDS[*]}" SIGINT
		else
			trap "kill ${PIDS[*]}; kill -KILL ${PIDS[*]}; R CMD BATCH --vanilla --slave --no-timing \"--args $verbose\" end.R /dev/tty" SIGINT
		fi
		
		# wait for the murder
		wait
	fi
fi