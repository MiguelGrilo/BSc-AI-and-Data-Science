package com.app.meals.ui.viewModels

import androidx.lifecycle.SavedStateHandle
import com.app.meals.FakeMealsRepository
import com.app.meals.MainDispatcherRule
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.test.runTest
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test

@OptIn(ExperimentalCoroutinesApi::class)
class DetailsViewModelTest {

    @get:Rule
    val mainDispatcherRule = MainDispatcherRule()

    @Test
    fun `init loads meal details based on mealId`() = runTest {
        val repository = FakeMealsRepository()
        val savedState = SavedStateHandle(mapOf("mealId" to "123"))

        val viewModel = DetailsViewModel(repository, savedState)

        val state = viewModel.detailsUiState
        assertTrue(state is DetailsUiState.Success)
        assertEquals("Tacos", (state as DetailsUiState.Success).meal.name)
    }
}