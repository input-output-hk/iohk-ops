#!/bin/sh
# goes from 3 regions to 4 regions topology and corresponding key index
                                                                                                                                                                                          
set +x                                                                                                                                                                                   
                                                                                                                                                                                        
#                                      previous -> new (4 regions)                                                                                                                     
mv node0.key mainnet/keys/key0.key   # IOHK c-a-1                                                                                                                                     
mv node1.key mainnet/keys/key1.key   # IOHK c-a-2                                                                                                                                    
mv node3.key mainnet/keys/key2.key   # Emurgo c-b-1                                                                                                                                 
mv node4.key mainnet/keys/key3.key   # Emurgo c-b-2                                                                                                                                 
mv node5.key mainnet/keys/key4.key   # CF c-c-1                                                                                                                                    
mv node6.key mainnet/keys/key5.key   # CF  c-c-2
mv node2.key mainnet/keys/key6.key   # IOHK c-a-3 -> c-d-1
