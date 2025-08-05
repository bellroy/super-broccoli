# Shopping Cart API Scripts

Individual scripts to test the shopping cart API endpoints. Make sure
the server is running on `localhost:8080` before running these
scripts.

## Available Scripts

### `create-cart.sh`

Creates a new cart with some initial items.

```bash
./create-cart.sh
```

### `get-cart.sh`

Retrieves a cart by its ID.

```bash
./get-cart.sh <cart_id>
```

### `add-item.sh`

Adds an item to an existing cart.

```bash
./add-item.sh <cart_id> <product_id> <quantity>
```

### `remove-item.sh`

Removes an item from a cart.

```bash
./remove-item.sh <cart_id> <product_id>
```

## Example Usage

1. First, create a cart:

   ```bash
   ./create-cart.sh
   ```

   Note the cart ID from the response.

2. Get the cart:

   ```bash
   ./get-cart.sh 1
   ```

3. Add an item:

   ```bash
   ./add-item.sh 1 1 3
   ```

4. Remove an item:
   ```bash
   ./remove-item.sh 1 2
   ```

## Available Products

- Product ID 1: "Slim Sleeve" ($119.00)
- Product ID 2: "Hide & Seek" ($129.00)
