#!/usr/bin/env bash
# Add an item to an existing cart
# Usage: ./add-item.sh <cart_id> <product_id> <quantity>

if [ $# -ne 3 ]; then
    echo "Usage: $0 <cart_id> <product_id> <quantity>"
    exit 1
fi

CART_ID=$1
PRODUCT_ID=$2
QUANTITY=$3

curl -X POST "http://localhost:8080/cart/$CART_ID/items" \
  -H "Content-Type: application/json" \
  -d "{
    \"productId\": $PRODUCT_ID,
    \"quantity\": $QUANTITY
  }" | jq '.'