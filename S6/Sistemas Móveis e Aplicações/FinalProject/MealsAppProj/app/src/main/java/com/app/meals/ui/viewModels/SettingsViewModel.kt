package com.app.meals.ui.viewModels

import androidx.lifecycle.ViewModel
import androidx.lifecycle.ViewModelProvider
import androidx.lifecycle.ViewModelProvider.AndroidViewModelFactory.Companion.APPLICATION_KEY
import androidx.lifecycle.viewmodel.initializer
import androidx.lifecycle.viewmodel.viewModelFactory
import com.app.meals.MealsApplication
import com.app.meals.data.repository.PreferencesRepository
import kotlinx.coroutines.flow.StateFlow


class SettingsViewModel(private val preferencesRepository: PreferencesRepository) : ViewModel() {

    val isDarkMode: StateFlow<Boolean> = preferencesRepository.isDarkMode

    fun toggleTheme(isDark: Boolean) {
        preferencesRepository.toggleDarkMode(isDark)
    }

    companion object {
        val Factory: ViewModelProvider.Factory = viewModelFactory {
            initializer {
                val application = (this[APPLICATION_KEY] as MealsApplication)
                SettingsViewModel(application.container.preferencesRepository)
            }
        }
    }
}