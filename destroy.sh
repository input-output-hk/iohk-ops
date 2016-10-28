read -p "Are you sure you want to destroy everything ([y/n]): " -n 1 -r
echo    
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

nixops destroy --all --confirm
nixops delete --all --confirm
