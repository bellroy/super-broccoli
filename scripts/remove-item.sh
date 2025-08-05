#!/usr/bin/env bash
# Remove an item from a cart
# Usage: ./remove-item.sh <cart_id> <product_id>

if [ $# -ne 2 ]; then
    echo "Usage: $0 <cart_id> <product_id>"
    exit 1
fi

CART_ID=$1
PRODUCT_ID=$2

curl -X DELETE "http://localhost:8080/cart/$CART_ID/items/$PRODUCT_ID" | jq '.'