package com.app.meals.ui.viewModels

import com.app.meals.data.repository.InMemoryPreferencesRepository
import org.junit.Assert.assertEquals
import org.junit.Test

class SettingsViewModelTest {

    @Test
    fun `toggleTheme updates repository`() {
        val repository = InMemoryPreferencesRepository()
        val viewModel = SettingsViewModel(repository)

        // Initial state defined in InMemoryPreferencesRepository
        assertEquals(true, viewModel.isDarkMode.value)

        // Toggle to Light Mode
        viewModel.toggleTheme(false)
        assertEquals(false, viewModel.isDarkMode.value)
    }
}