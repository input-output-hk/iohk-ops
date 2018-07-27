# Proxy configuration blocks
# https://www.terraform.io/docs/modules/usage.html#passing-providers-explicitly

provider "aws" {
}

provider "aws" {
  alias = "singapore"
}
