for i in $(ls -d */ | grep -v "conflict"); do
    cd $i;
        echo "Attempting to deploy $i"
        ./deploy.sh "$@";
        echo "Done with $i!"
    cd ..
done
