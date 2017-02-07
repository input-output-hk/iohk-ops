echo '{' > static/dht.json;
for i in {0..100}; do
  echo "  \"node$i\": \"$(./result/bin/cardano-dht-keygen -)\"," >> static/dht.json
done
echo '}' >> static/dht.json;
