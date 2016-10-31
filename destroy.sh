read -p "Are you sure you want to destroy cardano deployment? ([y/n]): " -n 1 -r
echo    
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

nixops destroy -d cardano --confirm
nixops delete -d cardano --confirm
