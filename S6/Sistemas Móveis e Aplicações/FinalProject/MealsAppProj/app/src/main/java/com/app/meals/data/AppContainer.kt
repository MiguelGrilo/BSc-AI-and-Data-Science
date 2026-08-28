package com.app.meals.data

import com.app.meals.data.repository.InMemoryPreferencesRepository
import com.app.meals.data.repository.MealsRepository
import com.app.meals.data.repository.NetworkMealRepository
import com.app.meals.data.repository.PreferencesRepository
import com.app.meals.network.MealsApiService
import com.jakewharton.retrofit2.converter.kotlinx.serialization.asConverterFactory
import kotlinx.serialization.json.Json
import okhttp3.MediaType.Companion.toMediaType
import retrofit2.Retrofit

/**
 * Dependency Injection container at the application level.
 */
interface AppContainer {
    val mealsRepository : MealsRepository
    val preferencesRepository: PreferencesRepository
}

/**
 * Implementation for the Dependency Injection container at the application level.
 * Variables are initialized lazily and the same instance is shared across the whole app.
 */
class DefaultAppContainer : AppContainer {
    private val baseUrl = "https://www.themealdb.com/api/json/v1/1/"

    /**
     * JSON configuration.
     * ignoreUnknownKeys = true is critical because the API returns many fields
     * (like ingredients and measurements) that are not mapped in our DTO.
     */
    private val jsonConfig = Json { ignoreUnknownKeys = true }

    /**
     * Use the Retrofit builder to build a retrofit object using a kotlinx.serialization converter
     */
    private val retrofit: Retrofit = Retrofit.Builder()
        .addConverterFactory(jsonConfig.asConverterFactory("application/json".toMediaType()))
        .baseUrl(baseUrl)
        .build()

    /**
     * Retrofit service object for creating api calls
     */
    private val retrofitService : MealsApiService by lazy {
        retrofit.create(MealsApiService::class.java)
    }

    /**
     * DI implementation for Meals repository
     */
    override val mealsRepository: MealsRepository by lazy {
        NetworkMealRepository(retrofitService)
    }

    /**
     * DI implementation for Preferences repository
     */
    override val preferencesRepository: PreferencesRepository by lazy {
        InMemoryPreferencesRepository()
    }
}