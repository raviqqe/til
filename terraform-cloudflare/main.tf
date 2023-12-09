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

data "cloudflare_api_token_permission_groups" "all" {}

resource "cloudflare_api_token" "audit_logs" {
  name = "logs_account"

  policy {
    permission_groups = [
      data.cloudflare_api_token_permission_groups.all.account["Access: Audit Logs Read"],
    ]
    resources = {
      "com.cloudflare.api.account.${local.cloudflare_account.id}" = "*"
    }
  }
}

output "account_premission_groups" {
  value = cloudflare_api_token_permission_groups.all.accout
}

output "user_premission_groups" {
  value = cloudflare_api_token_permission_groups.all.user
}
