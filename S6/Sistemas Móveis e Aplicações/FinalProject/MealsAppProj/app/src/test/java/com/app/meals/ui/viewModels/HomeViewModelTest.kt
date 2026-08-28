package com.app.meals.ui.viewModels

import com.app.meals.FakeMealsRepository
import com.app.meals.MainDispatcherRule
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.test.runTest
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test

@OptIn(ExperimentalCoroutinesApi::class)
class HomeViewModelTest {

    @get:Rule
    val mainDispatcherRule = MainDispatcherRule()

    @Test
    fun `init loads multiple meals and updates state to Success`() = runTest {
        val repository = FakeMealsRepository()
        val viewModel = HomeViewModel(repository)

        val state = viewModel.mealsUiState
        assertTrue(state is HomeUiState.Success)

        assertEquals(8, (state as HomeUiState.Success).meals.size)
    }

    @Test
    fun `API error changes state to Error`() = runTest {
        val repository = FakeMealsRepository().apply { shouldReturnError = true }
        val viewModel = HomeViewModel(repository)

        assertTrue(viewModel.mealsUiState is HomeUiState.Error)
    }
}