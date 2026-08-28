package com.app.meals.ui.viewModels

import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.ViewModelProvider.AndroidViewModelFactory.Companion.APPLICATION_KEY
import androidx.lifecycle.viewmodel.initializer
import androidx.lifecycle.viewmodel.viewModelFactory
import com.app.meals.MealsApplication
import com.app.meals.data.repository.PreferencesRepository
import kotlinx.coroutines.flow.StateFlow

class AppViewModel(private val preferencesRepository: PreferencesRepository) : ViewModel() {
    val isDarkMode: StateFlow<Boolean> = preferencesRepository.isDarkMode

    /**
     * Factory for [AppViewModel] that takes [PreferencesRepository] as a dependency.
     */
    companion object {
        val Factory: ViewModelProvider.Factory = viewModelFactory {
            initializer {
                val application = (this[APPLICATION_KEY] as MealsApplication)
                val preferencesRepository = application.container.preferencesRepository
                AppViewModel(preferencesRepository = preferencesRepository)
            }
        }
    }
}