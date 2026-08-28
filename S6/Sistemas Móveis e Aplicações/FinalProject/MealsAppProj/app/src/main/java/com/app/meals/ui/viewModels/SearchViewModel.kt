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
import com.app.meals.MealsApplication
import com.app.meals.data.repository.MealsRepository
import com.app.meals.model.Meal
import kotlinx.coroutines.launch

/**
 * The search screen UI state.
 */
sealed interface SearchUiState {
    object Idle : SearchUiState
    object Loading : SearchUiState
    data class Success(val meals: List<Meal>) : SearchUiState
    object Error : SearchUiState
}

class SearchViewModel(private val mealsRepository: MealsRepository) : ViewModel() {
    var uiState: SearchUiState by mutableStateOf(SearchUiState.Idle)
        private set

    var searchQuery by mutableStateOf("")
        private set

    var selectedCategory by mutableStateOf<String?>(null)
        private set

    // Fixed list of popular categories to quickly populate the filter chips
    val categories = listOf("Beef", "Chicken", "Dessert", "Pork", "Seafood", "Vegetarian")

    init {
        onCategorySelected("Beef")
    }

    fun onQueryChanged(query: String) {
        searchQuery = query
        // Clear selected category when the user types a new search query
        selectedCategory = null

        if (query.isBlank()) {
            uiState = SearchUiState.Idle
            return
        }
    }

    fun searchMeals() {
        if (searchQuery.isBlank()) return

        viewModelScope.launch {
            uiState = SearchUiState.Loading
            try {
                val results = mealsRepository.searchMealsByName(searchQuery)
                uiState = if (results.isEmpty()) SearchUiState.Idle else SearchUiState.Success(results)
            } catch (e: Exception) {
                uiState = SearchUiState.Error
            }
        }
    }

    fun onCategorySelected(category: String) {
        // Unselect the category if it was already selected
        if (selectedCategory == category) {
            selectedCategory = null
            uiState = SearchUiState.Idle
            return
        }

        selectedCategory = category
        // Clear the search query text when a category chip is clicked
        searchQuery = ""

        viewModelScope.launch {
            uiState = SearchUiState.Loading
            try {
                val results = mealsRepository.filterMealsByCategory(category)
                uiState = SearchUiState.Success(results)
            } catch (e: Exception) {
                uiState = SearchUiState.Error
            }
        }
    }

    /**
     * Factory for [SearchViewModel] that takes [MealsRepository] as a dependency
     */
    companion object{
        val Factory: ViewModelProvider.Factory = viewModelFactory {
            initializer {
                val application = (this[APPLICATION_KEY] as MealsApplication)
                val mealsRepository = application.container.mealsRepository
                SearchViewModel(mealsRepository = mealsRepository)
            }
        }
    }
}