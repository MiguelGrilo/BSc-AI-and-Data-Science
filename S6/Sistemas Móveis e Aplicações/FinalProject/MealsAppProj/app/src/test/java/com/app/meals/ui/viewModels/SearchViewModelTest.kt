package com.app.meals.ui.viewModels

import com.app.meals.FakeMealsRepository
import com.app.meals.MainDispatcherRule
import kotlinx.coroutines.ExperimentalCoroutinesApi
import kotlinx.coroutines.test.runTest
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Rule
import org.junit.Test

@OptIn(ExperimentalCoroutinesApi::class)
class SearchViewModelTest {

    @get:Rule
    val mainDispatcherRule = MainDispatcherRule()

    private lateinit var viewModel: SearchViewModel
    private lateinit var repository: FakeMealsRepository

    @Before
    fun setup() {
        repository = FakeMealsRepository()
        viewModel = SearchViewModel(repository)
    }

    @Test
    fun `init selects Beef category by default and loads data`() = runTest {
        assertEquals("Beef", viewModel.selectedCategory)
        assertTrue(viewModel.uiState is SearchUiState.Success)
    }

    @Test
    fun `onQueryChanged updates query and clears selected category`() {
        viewModel.onQueryChanged("Frango")
        assertEquals("Frango", viewModel.searchQuery)
        assertNull(viewModel.selectedCategory)
    }

    @Test
    fun `onCategorySelected with same category unselects it`() = runTest {
        viewModel.onCategorySelected("Beef") // Was already selected in init
        assertNull(viewModel.selectedCategory)
        assertTrue(viewModel.uiState is SearchUiState.Idle)
    }

    @Test
    fun `onCategorySelected clears search bar and loads new category`() = runTest {
        viewModel.onQueryChanged("Pizza") // Types something in the bar
        viewModel.onCategorySelected("Vegan") // Clicks on a chip

        assertEquals("", viewModel.searchQuery) // Bar should be cleared
        assertEquals("Vegan", viewModel.selectedCategory) // Chip updates
        assertTrue(viewModel.uiState is SearchUiState.Success)
    }

    @Test
    fun `searchMeals with API error returns Error state`() = runTest {
        repository.shouldReturnError = true
        viewModel.onQueryChanged("Bolo")
        viewModel.searchMeals()

        assertTrue(viewModel.uiState is SearchUiState.Error)
    }

    @Test
    fun `searchMeals with no results returns Idle state`() = runTest {
        repository.returnEmptyList = true
        viewModel.onQueryChanged("AlgoQueNaoExiste")
        viewModel.searchMeals()

        assertTrue(viewModel.uiState is SearchUiState.Idle)
    }
}