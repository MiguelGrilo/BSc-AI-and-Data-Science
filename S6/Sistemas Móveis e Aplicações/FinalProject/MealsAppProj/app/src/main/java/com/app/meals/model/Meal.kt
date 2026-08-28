package com.app.meals.model

data class Meal(
    val id: String,
    val name: String,
    val ingredients: List<String>,
    val measures: List<String>,
    val category: String? = null,
    val country: String? = null,
    val instructions: String? = null,
    val imageUrl: String? = null,
    val videoUrl: String? = null,
    val source: String? = null
)