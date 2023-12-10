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
  name = "til_audit_logs_test"

  policy {
    permission_groups = [
      data.cloudflare_api_token_permission_groups.all.account["Access: Audit Logs Read"],
    ]
    resources = {
      "com.cloudflare.api.account.${local.cloudflare_account.id}" = "*"
    }
  }
}

resource "cloudflare_r2_bucket" "til_test" {
  account_id = local.cloudflare_account.id
  name       = "til"
}

resource "cloudflare_api_token" "r2" {
  name = "til_r2_test"

  policy {
    permission_groups = [
      data.cloudflare_api_token_permission_groups.all.r2["Workers R2 Storage Bucket Item Read"],
      data.cloudflare_api_token_permission_groups.all.r2["Workers R2 Storage Bucket Item Write"]
    ]
    resources = {
      "com.cloudflare.edge.r2.bucket.${local.cloudflare_account.id}_default_${cloudflare_r2_bucket.til_test.id}" = "*"
    }
  }
}

output "account_premission_groups" {
  value = data.cloudflare_api_token_permission_groups.all.account
}

output "user_premission_groups" {
  value = data.cloudflare_api_token_permission_groups.all.user
}

output "r2_premission_groups" {
  value = data.cloudflare_api_token_permission_groups.all.r2
}

output "r2_bucket" {
  value = cloudflare_r2_bucket.til_test
}

output "r2_token" {
  value = cloudflare_api_token.r2.id
}
