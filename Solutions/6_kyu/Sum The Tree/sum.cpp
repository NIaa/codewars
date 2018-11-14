// Return the sum of all values in the tree, including the root
int sumTheTreeValues(node* root) {
  return root ? root -> value + sumTheTreeValues(root->left) + sumTheTreeValues(root->right) : 0;
}