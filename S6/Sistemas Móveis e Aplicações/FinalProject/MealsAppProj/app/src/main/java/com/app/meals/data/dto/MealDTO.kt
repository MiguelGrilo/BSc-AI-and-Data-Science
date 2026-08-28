package com.app.meals.data.dto

import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable

@Serializable
data class MealsListDTO (
    val meals: List<MealDTO>?
)

@Serializable
data class MealDTO(
    @SerialName(value = "idMeal")
    val id: String,
    @SerialName(value = "strMeal")
    val name: String,
    @SerialName(value = "strMealThumb")
    val imageUrl: String,
    @SerialName(value = "strCategory")
    val category: String? = null,
    @SerialName(value = "strArea")
    val country: String? = null,
    @SerialName(value = "strInstructions")
    val instructions: String? = null,
    @SerialName(value = "strYoutube")
    val youtubeUrl: String? = null,
    @SerialName(value = "strSource")
    val source: String? = null,

    @SerialName(value = "strIngredient1") val ingredient1: String? = null,
    @SerialName(value = "strIngredient2") val ingredient2: String? = null,
    @SerialName(value = "strIngredient3") val ingredient3: String? = null,
    @SerialName(value = "strIngredient4") val ingredient4: String? = null,
    @SerialName(value = "strIngredient5") val ingredient5: String? = null,
    @SerialName(value = "strIngredient6") val ingredient6: String? = null,
    @SerialName(value = "strIngredient7") val ingredient7: String? = null,
    @SerialName(value = "strIngredient8") val ingredient8: String? = null,
    @SerialName(value = "strIngredient9") val ingredient9: String? = null,
    @SerialName(value = "strIngredient10") val ingredient10: String? = null,
    @SerialName(value = "strIngredient11") val ingredient11: String? = null,
    @SerialName(value = "strIngredient12") val ingredient12: String? = null,
    @SerialName(value = "strIngredient13") val ingredient13: String? = null,
    @SerialName(value = "strIngredient14") val ingredient14: String? = null,
    @SerialName(value = "strIngredient15") val ingredient15: String? = null,
    @SerialName(value = "strIngredient16") val ingredient16: String? = null,
    @SerialName(value = "strIngredient17") val ingredient17: String? = null,
    @SerialName(value = "strIngredient18") val ingredient18: String? = null,
    @SerialName(value = "strIngredient19") val ingredient19: String? = null,
    @SerialName(value = "strIngredient20") val ingredient20: String? = null,

    @SerialName(value = "strMeasure1") val measure1: String? = null,
    @SerialName(value = "strMeasure2") val measure2: String? = null,
    @SerialName(value = "strMeasure3") val measure3: String? = null,
    @SerialName(value = "strMeasure4") val measure4: String? = null,
    @SerialName(value = "strMeasure5") val measure5: String? = null,
    @SerialName(value = "strMeasure6") val measure6: String? = null,
    @SerialName(value = "strMeasure7") val measure7: String? = null,
    @SerialName(value = "strMeasure8") val measure8: String? = null,
    @SerialName(value = "strMeasure9") val measure9: String? = null,
    @SerialName(value = "strMeasure10") val measure10: String? = null,
    @SerialName(value = "strMeasure11") val measure11: String? = null,
    @SerialName(value = "strMeasure12") val measure12: String? = null,
    @SerialName(value = "strMeasure13") val measure13: String? = null,
    @SerialName(value = "strMeasure14") val measure14: String? = null,
    @SerialName(value = "strMeasure15") val measure15: String? = null,
    @SerialName(value = "strMeasure16") val measure16: String? = null,
    @SerialName(value = "strMeasure17") val measure17: String? = null,
    @SerialName(value = "strMeasure18") val measure18: String? = null,
    @SerialName(value = "strMeasure19") val measure19: String? = null,
    @SerialName(value = "strMeasure20") val measure20: String? = null
)