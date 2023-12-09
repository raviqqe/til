terraform {
  required_providers {
    cloudflare = {
      source  = "cloudflare/cloudflare"
      version = "~> 4.0"
    }
  }
}

provider "cloudflare" {}

data "cloudflare_accounts" "main" {}

locals {
  cloudflare_account = data.cloudflare_accounts.main.accounts[0]
}

resource "cloudflare_workers_kv_namespace" "production" {
  account_id = local.cloudflare_account.id
  title      = "strawberry"
}
