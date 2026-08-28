package com.app.meals.network

import com.app.meals.data.dto.MealsListDTO
import retrofit2.http.GET
import retrofit2.http.Query

interface MealsApiService {
    @GET("random.php")
    suspend fun getRandomMeal(): MealsListDTO

    @GET("search.php")
    suspend fun searchMealsByName(@Query("s") query: String): MealsListDTO

    @GET("filter.php")
    suspend fun filterMealsByCategory(@Query("c") category: String): MealsListDTO

    @GET("lookup.php")
    suspend fun getMealById(@Query("i") id: String): MealsListDTO
}