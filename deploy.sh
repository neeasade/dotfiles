for i in $(ls); do
    cd $i;
    ./deploy.sh;
    cd ..
done
