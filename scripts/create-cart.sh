#!/usr/bin/env bash
# Create a new cart with initial items

curl -X POST "http://localhost:8080/cart" \
  -H "Content-Type: application/json" \
  -d '{
    "items": [
      {"productId": 1, "quantity": 2},
      {"productId": 2, "quantity": 1}
    ]
  }' | jq '.'