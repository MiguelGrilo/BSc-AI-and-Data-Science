package com.app.meals.data.mapper

import com.app.meals.data.dto.MealDTO
import com.app.meals.model.Meal

fun MealDTO.toDomain(): Meal {
    val ingredients = listOf(
        ingredient1, ingredient2, ingredient3, ingredient4, ingredient5,
        ingredient6, ingredient7, ingredient8, ingredient9, ingredient10,
        ingredient11, ingredient12, ingredient13, ingredient14, ingredient15,
        ingredient16, ingredient17, ingredient18, ingredient19, ingredient20
    ).isValid()
    val measures = listOf(
        measure1, measure2, measure3, measure4, measure5,
        measure6, measure7, measure8, measure9, measure10,
        measure11, measure12, measure13, measure14, measure15,
        measure16, measure17, measure18, measure19, measure20
    ).isValid()

    return Meal(
        id = this.id,
        name = this.name,
        ingredients = ingredients,
        measures = measures,
        category = this.category,
        country = this.country,
        instructions = this.instructions,
        imageUrl = this.imageUrl,
        videoUrl = this.youtubeUrl,
        source = this.source
    )
}

private fun List<String?>.isValid(): List<String> {
    return this.filterNotNull()
        .filter { it.isNotBlank() }
        .map { it.trim() }
}