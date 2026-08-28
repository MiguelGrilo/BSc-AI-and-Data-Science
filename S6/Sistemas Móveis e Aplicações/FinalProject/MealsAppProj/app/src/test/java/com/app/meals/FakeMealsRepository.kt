package com.app.meals

import com.app.meals.data.repository.MealsRepository
import com.app.meals.model.Meal
import java.io.IOException

class FakeMealsRepository : MealsRepository {
    var shouldReturnError = false
    var returnEmptyList = false

    val mockMeal = Meal("1", "Tacos", listOf("Carne"), listOf("1kg"))

    override suspend fun getRandomMeal(): Meal {
        if (shouldReturnError) throw IOException("Network Error")
        return mockMeal
    }

    override suspend fun searchMealsByName(query: String): List<Meal> {
        if (shouldReturnError) throw IOException("Network Error")
        if (returnEmptyList) return emptyList()
        return listOf(mockMeal, mockMeal)
    }

    override suspend fun filterMealsByCategory(category: String): List<Meal> {
        if (shouldReturnError) throw IOException("Network Error")
        return listOf(mockMeal)
    }

    override suspend fun getMealById(id: String): Meal {
        if (shouldReturnError) throw IOException("Network Error")
        return mockMeal
    }
}