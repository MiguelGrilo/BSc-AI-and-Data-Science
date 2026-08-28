package com.app.meals.data.repository

import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow

interface PreferencesRepository {
    val isDarkMode: StateFlow<Boolean>
    fun toggleDarkMode(isDark: Boolean)
}

class InMemoryPreferencesRepository : PreferencesRepository {
    private val _isDarkMode = MutableStateFlow(true)

    override val isDarkMode: StateFlow<Boolean> = _isDarkMode.asStateFlow()

    override fun toggleDarkMode(isDark: Boolean) {
        _isDarkMode.value = isDark
    }
}