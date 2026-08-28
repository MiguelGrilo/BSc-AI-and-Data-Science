package com.app.meals.data.repository

import com.app.meals.data.mapper.toDomain
import com.app.meals.model.Meal
import com.app.meals.network.MealsApiService

interface MealsRepository{
    suspend fun getRandomMeal() : Meal
    suspend fun searchMealsByName(query: String): List<Meal>
    suspend fun filterMealsByCategory(category: String): List<Meal>
    suspend fun getMealById(id: String): Meal
}

class NetworkMealRepository(
    private val mealsApiService: MealsApiService
) : MealsRepository {
    override suspend fun getRandomMeal(): Meal {
        val response = mealsApiService.getRandomMeal()
        return response.meals?.firstOrNull()?.toDomain() ?: throw Exception("No Meal Found")
    }

    override suspend fun searchMealsByName(query: String): List<Meal> {
        val response = mealsApiService.searchMealsByName(query)

        return response.meals
            ?.take(5)
            ?.map { it.toDomain() } ?: emptyList()
    }

    override suspend fun filterMealsByCategory(category: String): List<Meal> {
        val response = mealsApiService.filterMealsByCategory(category)

        return response.meals
            ?.shuffled()
            ?.take(5)
            ?.map { it.toDomain() } ?: emptyList()
    }

    override suspend fun getMealById(id: String): Meal {
        val response = mealsApiService.getMealById(id)
        return response.meals?.firstOrNull()?.toDomain() ?: throw Exception("Meal Details Not Found")
    }
}