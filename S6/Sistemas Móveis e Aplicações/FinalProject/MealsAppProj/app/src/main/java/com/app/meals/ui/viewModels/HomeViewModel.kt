package com.app.meals.ui.viewModels

import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.ViewModelProvider.AndroidViewModelFactory.Companion.APPLICATION_KEY
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.initializer
import androidx.lifecycle.viewmodel.viewModelFactory
import coil.network.HttpException
import com.app.meals.data.repository.MealsRepository
import com.app.meals.model.Meal
import com.app.meals.MealsApplication
import kotlinx.coroutines.async
import kotlinx.coroutines.awaitAll
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.launch
import java.io.IOException

/**
 * UI state for the Home screen
 */
sealed interface HomeUiState{
    data class Success(val meals: List<Meal>) : HomeUiState
    object Error : HomeUiState
    object Loading : HomeUiState
}

class HomeViewModel(private val mealsRepository: MealsRepository) : ViewModel() {
    /** The mutable State that stores the status of the most recent request */
    var mealsUiState: HomeUiState by mutableStateOf(HomeUiState.Loading)
        private set

    /**
     * Call getRandomMeal() on init so we can display status immediately.
     */
    init {
        getMultipleRandomMeals()
    }

    /**
     * Fetches a list of random meals from the API in parallel.
     * * It uses Kotlin Coroutines (async/awaitAll) to fire 8 simultaneous requests,
     * significantly optimizing the total response time compared to sequential calls.
     */
    fun getMultipleRandomMeals() {
        viewModelScope.launch {
            mealsUiState = HomeUiState.Loading

            mealsUiState = try {
                coroutineScope {
                    val deferredList = (1..8).map {
                        async { mealsRepository.getRandomMeal() }
                    }
                    val allMeals = deferredList.awaitAll()
                    HomeUiState.Success(allMeals)
                }
            } catch (e: IOException) {
                HomeUiState.Error
            } catch (e : HttpException){
                HomeUiState.Error
            }
        }
    }

    /**
     * Factory for [HomeViewModel] that takes [MealsRepository] as a dependency
     */
    companion object{
        val Factory: ViewModelProvider.Factory = viewModelFactory {
            initializer {
                val application = (this[APPLICATION_KEY] as MealsApplication)
                val mealsRepository = application.container.mealsRepository
                HomeViewModel(mealsRepository = mealsRepository)
            }
        }
    }
}