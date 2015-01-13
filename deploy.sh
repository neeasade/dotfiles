for i in $(ls -d */); do
    cd $i;
    ./deploy.sh;
    cd ..
done
