library(shiny)
library(here)
library(recipes)
library(ranger)
library(parsnip)

load(here("data", "un_smp_recipe.rda"))
load(here("data", "un_smp_lm.rda"))
load(here("data", "un_smp_rf.rda"))
