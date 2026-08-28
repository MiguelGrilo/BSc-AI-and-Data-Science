package com.app.meals.ui.viewModels

import android.net.http.HttpException
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.lifecycle.SavedStateHandle
import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.ViewModelProvider.AndroidViewModelFactory.Companion.APPLICATION_KEY
import androidx.lifecycle.createSavedStateHandle
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.initializer
import androidx.lifecycle.viewmodel.viewModelFactory
import com.app.meals.MealsApplication
import com.app.meals.data.repository.MealsRepository
import com.app.meals.model.Meal
import kotlinx.coroutines.launch
import java.io.IOException

/**
 * The meal detail screen UI state.
 */
sealed interface DetailsUiState {
    object Loading : DetailsUiState
    data class Success(val meal: Meal) : DetailsUiState
    object Error : DetailsUiState
}

class DetailsViewModel(
    private val mealsRepository: MealsRepository,
    savedStateHandle: SavedStateHandle
) : ViewModel(){

    /** The mutable State that stores the status of the most recent request. */
    var detailsUiState: DetailsUiState by mutableStateOf(DetailsUiState.Loading)
        private set

    /**
     * Automatically extracts the mealId from the navigation route arguments
     */
    private val mealId: String = checkNotNull(savedStateHandle["mealId"])

    init {
        getMealDetails()
    }

    /**
     * Gets meal details from the repository and updates [detailsUiState].
     */
    fun getMealDetails() {
        viewModelScope.launch {
            detailsUiState = DetailsUiState.Loading
            detailsUiState = try {
                val meal = mealsRepository.getMealById(mealId)
                DetailsUiState.Success(meal)
            } catch (e: IOException) {
                DetailsUiState.Error
            } catch (e: HttpException) {
                DetailsUiState.Error
            }
        }
    }

    /**
     * Factory for [DetailsViewModel] that takes [MealsRepository] as a dependency
     */
    companion object {
        val Factory: ViewModelProvider.Factory = viewModelFactory {
            initializer {
                val application = (this[APPLICATION_KEY] as MealsApplication)
                val mealsRepository = application.container.mealsRepository

                // Extracted the savedStateHandle from the creation extras
                val savedStateHandle = createSavedStateHandle()
                DetailsViewModel(
                    mealsRepository = mealsRepository,
                    savedStateHandle = savedStateHandle
                )
            }
        }
    }
}