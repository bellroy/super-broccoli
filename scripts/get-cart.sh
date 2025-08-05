#!/usr/bin/env bash
# Get a cart by ID
# Usage: ./get-cart.sh <cart_id>

if [ $# -eq 0 ]; then
    echo "Usage: $0 <cart_id>"
    exit 1
fi

CART_ID=$1

curl -X GET "http://localhost:8080/cart/$CART_ID" | jq '.'